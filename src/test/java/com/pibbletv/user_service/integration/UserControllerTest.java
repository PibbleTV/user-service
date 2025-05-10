package com.pibbletv.user_service.integration;


import com.fasterxml.jackson.databind.ObjectMapper;
import com.pibbletv.user_service.business.ImageLoader;
import com.pibbletv.user_service.business.converters.UserConverter;
import com.pibbletv.user_service.business.implementations.UserServiceImpl;
import com.pibbletv.user_service.business.interfaces.UserService;
import com.pibbletv.user_service.controller.UserController;
import com.pibbletv.user_service.domain.User;
import com.pibbletv.user_service.persistance.entities.UserEntity;
import com.pibbletv.user_service.persistance.repository.UserRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.reactive.AutoConfigureWebTestClient;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.web.reactive.server.WebTestClient;
import reactor.core.publisher.Mono;

import java.util.Base64;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

@SpringBootTest
@AutoConfigureWebTestClient
public class UserControllerTest {

    @Autowired
    private WebTestClient webTestClient;

    @Mock
    private UserRepository userRepository;

    @Mock
    private ImageLoader imageLoader;

    @InjectMocks
    private UserServiceImpl userService;


    @BeforeEach
    void setup() {
        Mockito.lenient().doAnswer(invocation -> {
            return new byte[]{1, 2, 3};
        }).when(imageLoader).load(anyString());
    }

    String base64Image = Base64.getEncoder().encodeToString(new byte[]{0x1, 0x2, 0x3});

    @Test
    void testSaveUser_shouldReturnOk_whenUserIsSavedSuccessfully() {

        String username = "testUser";

        when(userRepository.findByUsername(username)).thenReturn(Mono.empty());
        when(userRepository.save(any(UserEntity.class))).thenReturn(Mono.empty());
        when(userService.addUser(username)).thenReturn(Mono.empty());


        webTestClient.post()
                .uri(uriBuilder -> uriBuilder
                        .path("/user/addUser")
                        .queryParam("username", username)
                        .build())
                .exchange()
                .expectStatus().isOk();
    }

        @Test
    void testSaveUser_shouldReturnBadRequest_whenUserAlreadyExists() {

        String username = "testUser";

        when(userRepository.findByUsername(username)).thenReturn(Mono.error(new RuntimeException("User already exists")));
        when(userService.addUser(username)).thenThrow(new RuntimeException("User already exists"));

        webTestClient.post()
                .uri(uriBuilder -> uriBuilder
                        .path("/user/addUser")
                        .queryParam("username", username)
                        .build())
                .exchange()
                .expectStatus().isBadRequest();
    }

    @Test
    void testUpdateUser_shouldReturnOk_whenUserExists() {
        User user = new User(1L, "updatedUser", base64Image, base64Image, false);

        when(userRepository.existsById(1L)).thenReturn(Mono.just(true));
        when(userRepository.save(any(UserEntity.class))).thenReturn(Mono.empty());
        when(userService.updateUser(user)).thenReturn(Mono.empty());

        webTestClient.put()
                .uri("/user/updateUser")
                .contentType(MediaType.APPLICATION_JSON)
                .bodyValue(user)
                .exchange()
                .expectStatus().isOk();
    }

    @Test
    void testUpdateUser_shouldReturnBadRequest_whenUserIdIsNull() {
        User user = new User(null, "userWithoutId", base64Image, base64Image, false);

        when(userService.updateUser(user)).thenReturn(Mono.error(new IllegalArgumentException("Cannot update user without ID")));

        webTestClient.put()
                .uri("/user/updateUser")
                .contentType(MediaType.APPLICATION_JSON)
                .bodyValue(user)
                .exchange()
                .expectStatus().isBadRequest();
    }

    @Test
    void testUpdateUser_shouldReturnBadRequest_whenUserDoesNotExist() {
        User user = new User(99L, "nonExistingUser", base64Image, base64Image, false);

        when(userRepository.existsById(99L)).thenReturn(Mono.just(false));
        when(userService.updateUser(user)).thenReturn(Mono.error(new IllegalArgumentException("User does not exist")));

        webTestClient.put()
                .uri("/user/updateUser")
                .contentType(MediaType.APPLICATION_JSON)
                .bodyValue(user)
                .exchange()
                .expectStatus().isBadRequest();
    }

    @Test
    @WithMockUser(roles = "admin")
    void testBanUser_shouldReturnOk_whenUserIsFoundAndNotBanned() {
        String username = "userToBan";

        UserEntity user = new UserEntity(1L, username, null, null, false);

        when(userRepository.findByUsername(username)).thenReturn(Mono.just(user));
        when(userRepository.save(any(UserEntity.class))).thenReturn(Mono.just(user));
        when(userService.banUser(username)).thenReturn(Mono.empty());

        webTestClient.put()
                .uri("/user/banUser?username=" + username)
                .exchange()
                .expectStatus().isOk();
    }

    @Test
    @WithMockUser(roles = "admin")
    void testBanUser_shouldReturnBadRequest_whenUserAlreadyBanned() {
        String username = "alreadyBannedUser";

        UserEntity user = new UserEntity(1L, username, null, null, true);

        when(userRepository.findByUsername(username)).thenReturn(Mono.just(user));
        when(userService.banUser(username)).thenReturn(Mono.error(new IllegalStateException("User is already banned")));

        webTestClient.put()
                .uri("/user/banUser?username=" + username)
                .exchange()
                .expectStatus().isBadRequest();
    }

    @Test
    @WithMockUser(roles = "admin")
    void testBanUser_shouldReturnBadRequest_whenUserNotFound() {
        String username = "nonExistentUser";

        when(userRepository.findByUsername(username)).thenReturn(Mono.error(new RuntimeException("User not found")));
        when(userService.banUser(username)).thenReturn(Mono.error(new RuntimeException("User not found")));

        webTestClient.put()
                .uri("/user/banUser?username=" + username)
                .exchange()
                .expectStatus().isBadRequest();
    }

    @Test
    void testBanUser_shouldReturnForbidden_whenUserIsNotAdmin() {
        webTestClient.put()
                .uri("/user/banUser?username=someUser")
                .exchange()
                .expectStatus().isForbidden();
    }

    @Test
    @WithMockUser(roles = "admin")
    void testUnbanUser_shouldReturnOk_whenUserIsBanned() {
        String username = "bannedUser";

        UserEntity user = new UserEntity(1L, username, null, null, true);

        when(userRepository.findByUsername(username)).thenReturn(Mono.just(user));
        when(userRepository.save(any(UserEntity.class))).thenReturn(Mono.empty());
        when(userService.unbanUser(username)).thenReturn(Mono.empty());

        webTestClient.put()
                .uri("/user/unbanUser?username=" + username)
                .exchange()
                .expectStatus().isOk();
    }

    @Test
    @WithMockUser(roles = "admin")
    void testUnbanUser_shouldReturnBadRequest_whenUserIsNotBanned() {
        String username = "notBannedUser";

        UserEntity user = new UserEntity(1L, username, null, null, false);

        when(userRepository.findByUsername(username)).thenReturn(Mono.just(user));
        when(userService.unbanUser(username)).thenReturn(Mono.error(new IllegalStateException("User is not banned")));

        webTestClient.put()
                .uri("/user/unbanUser?username=" + username)
                .exchange()
                .expectStatus().isBadRequest();
    }

    @Test
    @WithMockUser(roles = "admin")
    void testUnbanUser_shouldReturnBadRequest_whenUserNotFound() {
        String username = "nonExistentUser";

        when(userRepository.findByUsername(username)).thenReturn(Mono.error(new RuntimeException("User not found")));
        when(userService.getUser(username)).thenReturn(Mono.error(new RuntimeException("User not found")));

        webTestClient.put()
                .uri("/user/unbanUser?username=" + username)
                .exchange()
                .expectStatus().isBadRequest();
    }

    @Test
    void testUnbanUser_shouldReturnForbidden_whenUserIsNotAdmin() {
        webTestClient.put()
                .uri("/user/unbanUser?username=someUser")
                .exchange()
                .expectStatus().isForbidden();
    }

    @Test
    void testGetUser_shouldReturnUser_whenFound() {
        String username = "existingUser";

        UserEntity entity = new UserEntity(1L, username, null, null, false);
        User user = UserConverter.convertToObject(entity);

        when(userRepository.findByUsername(username)).thenReturn(Mono.just(entity));
        when(userService.getUser(username)).thenReturn(Mono.just(user));

        webTestClient.get()
                .uri("/user/getUser?username=" + username)
                .exchange()
                .expectStatus().isOk()
                .expectBody()
                .jsonPath("$.username").isEqualTo(username);
    }

    @Test
    void testGetUser_shouldReturnNotFound_whenUserDoesNotExist() {
        String username = "nonExistingUser";

        when(userRepository.findByUsername(username)).thenReturn(Mono.error(new RuntimeException("User not found")));
        when(userService.getUser(username)).thenReturn(Mono.error(new RuntimeException("User not found")));

        webTestClient.get()
                .uri("/user/getUser?username=" + username)
                .exchange()
                .expectStatus().isNotFound();
    }
}


