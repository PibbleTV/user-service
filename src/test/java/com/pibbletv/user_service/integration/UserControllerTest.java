package com.pibbletv.user_service.integration;


import com.fasterxml.jackson.databind.ObjectMapper;
import com.pibbletv.user_service.business.implementations.UserServiceImpl;
import com.pibbletv.user_service.business.interfaces.UserService;
import com.pibbletv.user_service.controller.UserController;
import com.pibbletv.user_service.persistance.entities.UserEntity;
import com.pibbletv.user_service.persistance.repository.UserRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.reactive.AutoConfigureWebTestClient;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.reactive.server.WebTestClient;
import reactor.core.publisher.Mono;

import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@SpringBootTest
@AutoConfigureWebTestClient
public class UserControllerTest {

    @Autowired
    private WebTestClient webTestClient;

    @Mock
    private UserService userService;

    @Test
    void testSaveUser_shouldReturnOk_whenUserIsSavedSuccessfully() {

        String username = "testUser";

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

        when(userService.addUser(username)).thenThrow(new RuntimeException("User already exists"));

        webTestClient.post()
                .uri(uriBuilder -> uriBuilder
                        .path("/user/addUser")
                        .queryParam("username", username)
                        .build())
                .exchange()
                .expectStatus().isBadRequest();
    }


}
