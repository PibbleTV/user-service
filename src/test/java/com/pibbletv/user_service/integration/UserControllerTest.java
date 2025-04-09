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

    @Autowired
    private UserService userService;


    @Test
    void testSaveUser_shouldReturnOk_whenUserIsSavedSuccessfully() {

        String userId = "123";
        String username = "testUser";
        Map<String, Object> requestData = new HashMap<>();
        requestData.put("userId", userId);
        requestData.put("username", username);

        webTestClient.post()
                .uri("/user/saveUser")
                .contentType(org.springframework.http.MediaType.APPLICATION_JSON)
                .bodyValue(requestData)
                .exchange()
                .expectStatus().isOk();
    }

    @Test
    void testSaveUser_shouldReturnBadRequest_whenUserAlreadyExists() {

        String userId = "123";
        String username = "testUser";
        Map<String, Object> requestData = new HashMap<>();
        requestData.put("userId", userId);
        requestData.put("username", username);

        webTestClient.post()
                .uri("/user/saveUser")
                .contentType(org.springframework.http.MediaType.APPLICATION_JSON)
                .bodyValue(requestData)
                .exchange()
                .expectStatus().isBadRequest();
    }

}
