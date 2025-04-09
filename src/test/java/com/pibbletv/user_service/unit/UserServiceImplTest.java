package com.pibbletv.user_service.unit;

import com.pibbletv.user_service.business.ImageLoader;
import com.pibbletv.user_service.persistance.entities.UserEntity;
import com.pibbletv.user_service.persistance.repository.UserRepository;
import com.pibbletv.user_service.domain.User;
import com.pibbletv.user_service.business.implementations.UserServiceImpl;
import com.pibbletv.user_service.business.converters.UserConverter;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.*;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.mockito.Mockito.*;

@ExtendWith(org.mockito.junit.jupiter.MockitoExtension.class)
class UserServiceImplTest {


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

    @Test
    void testSaveUser_NewUser_SavesSuccessfully() {
        String userId = "123";
        String username = "testuser";

        when(userRepository.existsById(Long.parseLong(userId))).thenReturn(Mono.just(false));
        when(userRepository.save(any(UserEntity.class))).thenReturn(Mono.just(new UserEntity()));

        StepVerifier.create(userService.saveUser(userId, username))
                .verifyComplete();

        verify(userRepository).save(argThat(user ->
                user.getId().equals(123L) &&
                        user.getUsername().equals("testuser")
        ));
    }


    @Test
    void testGetUser_ExistingUser_ReturnsUser() {
        String userId = "123";
        UserEntity entity = new UserEntity();
        entity.setId(123L);
        entity.setUsername("testuser");

        when(userRepository.findById(123L)).thenReturn(Mono.just(entity));

        User expected = UserConverter.convertToObject(entity);

        StepVerifier.create(userService.getUser(userId))
                .expectNextMatches(user -> user.getId().equals(expected.getId()) && user.getUsername().equals(expected.getUsername()))
                .verifyComplete();
    }

    @Test
    void testSaveUser_ExistingUser_DoesNotSaveAgain() {
        String userId = "123";
        String username = "testuser";

        when(userRepository.existsById(Long.parseLong(userId))).thenReturn(Mono.just(true));

        StepVerifier.create(userService.saveUser(userId, username))
                .expectErrorMatches(throwable -> throwable instanceof RuntimeException &&
                        throwable.getMessage().equals("User already exists"))
                .verify();

        verify(userRepository, never()).save(any());
    }

    @Test
    void testGetUser_NotFound_ThrowsError() {
        when(userRepository.findById(123L)).thenReturn(Mono.empty());

        StepVerifier.create(userService.getUser("123"))
                .expectErrorMatches(throwable -> throwable instanceof RuntimeException &&
                        throwable.getMessage().equals("User not found"))
                .verify();
    }
}
